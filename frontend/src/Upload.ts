// See `https://github.com/origin-space/originui/blob/main/docs/use-file-upload.md'.

import type { FileWithPreview } from "./hooks/use-file-upload"

// Type for tracking upload progress.
export type Progress = {
  fileId: string
  progress: number
  completed: boolean
  error?: string
}

// Function that uploads a single file.
async function upload(
  file: File,
  adress: string,
  setUploadProgress: React.Dispatch<React.SetStateAction<Progress[]>>,
): Promise<{ url: string }> {
  return new Promise(async (resolve, reject) => {
    try {
      // Create FormData.
      const formData = new FormData()
      formData.append('file', file)

      // Create XMLHttpRequest to track progress.
      const xhr = new XMLHttpRequest()

      // Track upload progress.
      xhr.upload.addEventListener('progress', (event) => {
	if (event.lengthComputable) {
	  const progressPercent = Math.round((event.loaded / event.total) * 100)
	  // Update progress state for this file.
	  setUploadProgress(prev => prev.map(item =>
	    item.fileId === file.name ? { ...item, progress: progressPercent } : item
	  ))
	}
      })

      // Handle completion.
      xhr.addEventListener('load', () => {
	if (xhr.status >= 200 && xhr.status < 300) {
	  const response = JSON.parse(xhr.responseText)
	  // Mark as completed.
	  setUploadProgress(prev => prev.map(item =>
	    item.fileId === file.name ? { ...item, completed: true } : item
	  ))
	  resolve(response)
	} else {
	  // Handle error.
	  setUploadProgress(prev => prev.map(item =>
	    item.fileId === file.name ? { ...item, error: 'Upload failed' } : item
	  ))
	  reject(new Error('Upload failed'))
	}
      })

      // Handle error.
      xhr.addEventListener('error', () => {
	setUploadProgress(prev => prev.map(item =>
	  item.fileId === file.name ? { ...item, error: 'Network error' } : item
	))
	reject(new Error('Network error'))
      })

      // Open and send the request.
      xhr.open('POST', adress, true)
      xhr.send(formData)
    } catch (error) {
      reject(error)
    }
  })
}

// Queue newly added files to track their progress.
export function queue(
  setUploadProgress: React.Dispatch<React.SetStateAction<Progress[]>>,
): (files: FileWithPreview[]) => void {
  return (files: FileWithPreview[]) => {
    // Initialize progress tracking for each new file.
    const newProgressItems = files.map((file) => {
      console.log(file)
      return {
	fileId: file.id,
	progress: 0,
	completed: false,
      }})

    // Add new progress items to state.
    setUploadProgress(prev => [...prev, ...newProgressItems])
  }
}

// Start the upload.
export function start(
  files: FileWithPreview[],
  adress: string,
  setUploadProgress: React.Dispatch<React.SetStateAction<Progress[]>>,
) {
  files.forEach(file => {
    if (file.file instanceof File) {
      upload(file.file, adress, setUploadProgress)
	.then(response => {
	  console.log('Upload successful:', response.url)
	})
	.catch(error => {
	  console.error('Upload failed:', error)
	})
    }
  })
}

// Convenience combination of queue and start.
export function queueAndStart(
  adress: string,
  setUploadProgress: React.Dispatch<React.SetStateAction<Progress[]>>,
): (files: FileWithPreview[]) => void {
  return (files: FileWithPreview[]) => {
    queue(setUploadProgress)(files)
    start(files, adress, setUploadProgress)
  }
}

// Remove progress tracking for the file.
export function cancel(
  fileId: string,
  setUploadProgress: React.Dispatch<React.SetStateAction<Progress[]>>,
) {
  setUploadProgress(prev => prev.filter(item => item.fileId !== fileId))
}

